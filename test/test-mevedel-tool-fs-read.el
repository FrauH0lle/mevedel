;;; test-mevedel-tool-fs-read.el -- Tests for Read file-system tools -*- lexical-binding: t -*-

;;; Commentary:

;; Behavioral coverage for the Read text/media owner.

;;; Code:

(require 'gptel-request)
(require 'mevedel-tool-fs-read)
(require 'mevedel-structs)
(require 'mevedel-agents)
(require 'mevedel-execution)
(require 'mevedel-reminders)
(require 'mevedel-system)
(require 'mevedel-view)
(require 'mevedel-resource)
(require 'mevedel-workspace)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))

(mevedel-deftest mevedel-tool-fs-read--with-local-media-source ()
  ,test
  (test)
  :doc "keeps local converter inputs unchanged"
  (let ((source (make-temp-file "mevedel-media-local-" nil ".png")))
    (unwind-protect
        (should (equal source
                       (mevedel-tool-fs-read--with-local-media-source
                        source #'identity)))
      (delete-file source)))

  :doc "copies remote converter inputs locally and deletes the copy"
  (let ((source (make-temp-file "mevedel-media-remote-" nil ".png"
                                "remote-media"))
        copied)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("media")
                                              (let ((remote (format "/mevedelmock:media:%s" source)))
                                                (setq copied
                                                      (mevedel-tool-fs-read--with-local-media-source
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
  (let ((source (make-temp-file "mevedel-media-oversized-" nil ".pdf"
                                "oversized"))
        (mevedel-tool-fs-read--remote-media-copy-max-bytes 1)
        called)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp '("media")
                                              (should-error
                                               (mevedel-tool-fs-read--with-local-media-source
                                                (format "/mevedelmock:media:%s" source)
                                                (lambda (_local) (setq called t))))
                                              (should-not called))
      (delete-file source))))

;;
;;; Binary extension detection

(mevedel-deftest mevedel-tool-fs-read--binary-extension-p ()
  ,test
  (test)
  :doc "detects common binary extensions"
  (should (mevedel-tool-fs-read--binary-extension-p "image.png"))
  (should (mevedel-tool-fs-read--binary-extension-p "archive.zip"))
  (should (mevedel-tool-fs-read--binary-extension-p "binary.exe"))
  (should (mevedel-tool-fs-read--binary-extension-p "doc.pdf"))
  :doc "rejects text file extensions"
  (should-not (mevedel-tool-fs-read--binary-extension-p "code.el"))
  (should-not (mevedel-tool-fs-read--binary-extension-p "readme.md"))
  (should-not (mevedel-tool-fs-read--binary-extension-p "config.json"))
  (should-not (mevedel-tool-fs-read--binary-extension-p "style.css"))
  :doc "case insensitive"
  (should (mevedel-tool-fs-read--binary-extension-p "IMAGE.PNG"))
  (should (mevedel-tool-fs-read--binary-extension-p "file.JPG"))
  :doc "handles files without extension"
  (should-not (mevedel-tool-fs-read--binary-extension-p "Makefile"))
  (should-not (mevedel-tool-fs-read--binary-extension-p ".gitignore")))

;;
;;; Media helpers

(defconst test-mevedel-tool-fs-read--png-bytes
  (unibyte-string #x89 ?P ?N ?G ?\r ?\n #x1a ?\n)
  "Minimal PNG bytes used by Read media tests.")

(defun test-mevedel-tool-fs-read--write-bytes (path bytes)
  "Write unibyte BYTES to PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert bytes)
    (let ((coding-system-for-write 'binary))
      (write-region nil nil path nil 'silent))))

(mevedel-deftest mevedel-tool-fs-read-media-mime-type ()
  ,test
  (test)
  :doc "detects supported media MIME types"
  (should (equal "image/png" (mevedel-tool-fs-read-media-mime-type "a.png")))
  (should (equal "image/jpeg" (mevedel-tool-fs-read-media-mime-type "a.jpg")))
  (should (equal "image/jpeg" (mevedel-tool-fs-read-media-mime-type "a.JPEG")))
  (should (equal "image/gif" (mevedel-tool-fs-read-media-mime-type "a.gif")))
  (should (equal "image/webp" (mevedel-tool-fs-read-media-mime-type "a.webp")))
  (should (equal "application/pdf"
                 (mevedel-tool-fs-read-media-mime-type "doc.pdf")))
  :doc "returns nil for unsupported binary and text files"
  (should-not (mevedel-tool-fs-read-media-mime-type "archive.zip"))
  (should-not (mevedel-tool-fs-read-media-mime-type "readme.md")))

(mevedel-deftest mevedel-tool-fs-read--parse-pages ()
  ,test
  (test)
  :doc "parses single page"
  (should (equal '(3 . 3) (mevedel-tool-fs-read--parse-pages "3")))
  :doc "parses closed range"
  (should (equal '(1 . 5) (mevedel-tool-fs-read--parse-pages "1-5")))
  :doc "parses open range with 20-page cap"
  (should (equal '(3 . 22) (mevedel-tool-fs-read--parse-pages "3-")))
  :doc "rejects invalid syntax"
  (should-error (mevedel-tool-fs-read--parse-pages "x-y") :type 'error)
  :doc "rejects descending ranges"
  (should-error (mevedel-tool-fs-read--parse-pages "5-2") :type 'error)
  :doc "rejects ranges over 20 pages"
  (should-error (mevedel-tool-fs-read--parse-pages "1-21") :type 'error))

(mevedel-deftest mevedel-tool-fs-read--bounded-pdf-page-range ()
  ,test
  (test)
  :doc "limits open-ended ranges to the actual PDF page count"
  (cl-letf (((symbol-function 'mevedel-tool-fs-read--pdf-page-count)
             (lambda (_path) 1)))
    (should (equal '(1 . 1)
                   (mevedel-tool-fs-read--bounded-pdf-page-range
                    "/tmp/doc.pdf" "1-"))))
  :doc "limits closed ranges to the actual PDF page count"
  (cl-letf (((symbol-function 'mevedel-tool-fs-read--pdf-page-count)
             (lambda (_path) 1)))
    (should (equal '(1 . 1)
                   (mevedel-tool-fs-read--bounded-pdf-page-range
                    "/tmp/doc.pdf" "1-5"))))
  :doc "falls back to the 20-page cap when the page count is unavailable"
  (cl-letf (((symbol-function 'mevedel-tool-fs-read--pdf-page-count)
             (lambda (_path) nil)))
    (should (equal '(3 . 22)
                   (mevedel-tool-fs-read--bounded-pdf-page-range
                    "/tmp/doc.pdf" "3-"))))
  :doc "rejects ranges that start after the last page"
  (cl-letf (((symbol-function 'mevedel-tool-fs-read--pdf-page-count)
             (lambda (_path) 2)))
    (let ((err (should-error
                (mevedel-tool-fs-read--bounded-pdf-page-range
                 "/tmp/doc.pdf" "3-4")
                :type 'error)))
      (should (string-match-p "starts after last page" (cadr err))))))

(mevedel-deftest mevedel-tool-fs-read-large-pdf-p ()
  ,test
  (test)
  :doc "large when page count exceeds one Read page chunk"
  (let ((tmp (make-temp-file "mevedel-large-pdf-" nil ".pdf" "%PDF-1.4\n")))
    (unwind-protect
        (cl-letf (((symbol-function 'mevedel-tool-fs-read--pdf-page-count)
                   (lambda (_path) (1+ mevedel-tool-fs-read--max-pages))))
          (should (mevedel-tool-fs-read-large-pdf-p tmp)))
      (delete-file tmp)))
  :doc "large when page count is unavailable but attachment size is high"
  (let ((tmp (make-temp-file "mevedel-large-pdf-" nil ".pdf" "%PDF-1.4\npayload")))
    (unwind-protect
        (let ((mevedel-tool-fs-read--large-attachment-reminder-bytes 1))
          (cl-letf (((symbol-function 'mevedel-tool-fs-read--pdf-page-count)
                     (lambda (_path) nil)))
            (should (mevedel-tool-fs-read-large-pdf-p tmp))))
      (delete-file tmp)))
  :doc "small PDFs do not trigger bounded-page guidance"
  (let ((tmp (make-temp-file "mevedel-large-pdf-" nil ".pdf" "%PDF-1.4\n")))
    (unwind-protect
        (let ((mevedel-tool-fs-read--large-attachment-reminder-bytes 1024))
          (cl-letf (((symbol-function 'mevedel-tool-fs-read--pdf-page-count)
                     (lambda (_path) 3)))
            (should-not (mevedel-tool-fs-read-large-pdf-p tmp))))
      (delete-file tmp))))

(mevedel-deftest mevedel-tool-fs-read--normalize-read-args ()
  ,test
  (test)
  :doc "treats empty pages and zero optional integers as absent"
  (let ((normalized (mevedel-tool-fs-read--normalize-read-args
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
  (let ((normalized (mevedel-tool-fs-read--normalize-read-args
                     '(:file_path "relative.png"
                       :offset 3 :limit 12 :max_height 900
                       :max_tokens 2000))))
    (should (equal (expand-file-name "relative.png")
                   (plist-get normalized :file_path)))
    (should (equal 3 (plist-get normalized :offset)))
    (should (equal 12 (plist-get normalized :limit)))
    (should (equal 900 (plist-get normalized :max_height)))
    (should (equal 2000 (plist-get normalized :max_tokens)))))

(mevedel-deftest mevedel-tool-fs-read--media-file ()
  ,test
  (test)
  :doc "remote image conversion uses temporary local input and output paths"
  (let ((root (file-name-as-directory
               (make-temp-file "mevedel-remote-media-root-" t)))
        converter-input converter-output)
    (unwind-protect
        (progn
          (test-mevedel-tool-fs-read--write-bytes
           (file-name-concat root "image.png")
           test-mevedel-tool-fs-read--png-bytes)
          (mevedel-test--with-local-shell-tramp '("media")
            (let* ((remote-root (format "/mevedelmock:media:%s" root))
                   (remote-path (file-name-concat remote-root "image.png"))
                   (workspace (mevedel-workspace--create
                               :type 'project :id "remote-media"
                               :root remote-root :name "remote-media"))
                   (session (mevedel-session-create "main" workspace))
                   (mevedel--session session))
              (cl-letf
                  (((symbol-function 'gptel--model-capable-p)
                    (lambda (cap &optional _model) (eq cap 'media)))
                   ((symbol-function 'gptel--model-mime-capable-p)
                    (lambda (mime &optional _model)
                      (equal mime "image/png")))

                   ((symbol-function 'mevedel-tool-fs-read--imagemagick-command)
                    (lambda () "magick"))
                   ((symbol-function 'mevedel-execution-run-helper)
                    (lambda (_name command _read-paths _writable-roots
                                   &rest keys)
                      (setq converter-input (nth 1 command)
                            converter-output (car (last command)))
                      (should-not (file-remote-p converter-input))
                      (should-not (file-remote-p converter-output))
                      (should-not (plist-get keys :session))
                      (test-mevedel-tool-fs-read--write-bytes
                       converter-output test-mevedel-tool-fs-read--png-bytes)
                      '(:exit-code 0 :output ""))))
                (let* ((result (mevedel-tool-fs-read--media-file
                                remote-path '(:max_width 64)))
                       (body (plist-get result :result))
                       (media (car (plist-get result :media))))
                  (should (string-match-p
                           (regexp-quote
                            (file-name-concat root "image.png"))
                           body))
                  (should-not (string-match-p "/mevedelmock:" body))
                  (should-not (string-match-p "mevedel-media-" body))
                  (should (equal (file-name-concat root "image.png")
                                 (plist-get media :path)))))))
          (should-not (file-exists-p converter-input))
          (should-not (file-exists-p converter-output)))
      (delete-directory root t))))

(mevedel-deftest mevedel-tool-fs-read--pdf-pages ()
  ,test
  (test)
  :doc "remote PDF conversion uses local Poppler input and removes artifacts"
  (let ((root (file-name-as-directory
               (make-temp-file "mevedel-remote-pdf-root-" t)))
        transient-paths)
    (unwind-protect
        (progn
          (write-region "%PDF-1.4\n" nil
                        (file-name-concat root "document.pdf") nil 'silent)
          (mevedel-test--with-local-shell-tramp '("pdf")
            (let* ((remote-root (format "/mevedelmock:pdf:%s" root))
                   (remote-path
                    (file-name-concat remote-root "document.pdf"))
                   (workspace (mevedel-workspace--create
                               :type 'project :id "remote-pdf"
                               :root remote-root :name "remote-pdf"))
                   (session (mevedel-session-create "main" workspace))
                   (mevedel--session session))
              (cl-letf
                  (((symbol-function 'executable-find)
                    (lambda (name &optional _remote)
                      (and (member name '("pdfinfo" "pdftoppm")) name)))
                   ((symbol-function 'gptel--model-capable-p)
                    (lambda (cap &optional _model) (eq cap 'media)))
                   ((symbol-function 'gptel--model-mime-capable-p)
                    (lambda (mime &optional _model)
                      (equal mime "image/png")))

                   ((symbol-function 'mevedel-execution-run-helper)
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
                           (test-mevedel-tool-fs-read--write-bytes
                            output test-mevedel-tool-fs-read--png-bytes)
                           '(:exit-code 0 :output "")))
                        (_ '(:exit-code 1 :output "unexpected"))))))
                (let* ((result (mevedel-tool-fs-read--pdf-pages
                                remote-path "1" nil))
                       (body (plist-get result :result))
                       (media (car (plist-get result :media))))
                  (should (string-match-p
                           (regexp-quote
                            (file-name-concat root "document.pdf"))
                           body))
                  (should-not (string-match-p "/mevedelmock:" body))
                  (should (equal (file-name-concat root "document.pdf")
                                 (plist-get media :path)))))))
          (dolist (path transient-paths)
            (should-not (file-exists-p path))))
      (delete-directory root t))))

;;
;;; Blocked device detection

(mevedel-deftest mevedel-tool-fs-read--blocked-device-p ()
  ,test
  (test)
  :doc "blocks dangerous device paths"
  (should (mevedel-tool-fs-read--blocked-device-p "/dev/zero"))
  (should (mevedel-tool-fs-read--blocked-device-p "/dev/random"))
  (should (mevedel-tool-fs-read--blocked-device-p "/dev/stdin"))
  (should (mevedel-tool-fs-read--blocked-device-p "/dev/fd/0"))
  :doc "blocks proc fd aliases"
  (should (mevedel-tool-fs-read--blocked-device-p "/proc/self/fd/0"))
  (should (mevedel-tool-fs-read--blocked-device-p "/proc/1234/fd/1"))
  :doc "allows safe devices and regular paths"
  (should-not (mevedel-tool-fs-read--blocked-device-p "/dev/null"))
  (should-not (mevedel-tool-fs-read--blocked-device-p "/tmp/file.txt"))
  (should-not (mevedel-tool-fs-read--blocked-device-p "/proc/self/status")))

;;
;;; Read handler

(mevedel-deftest mevedel-tool-fs-read
  ()
  ,test
  (test)
  :doc "reads local and artifact resources without exposing backing paths"
  (let* ((save-path (make-temp-file "mevedel-fs-resource-" t))
         (session (mevedel-session--create
                   :authority-mode 'pid-lock :save-path save-path))
         (local-root (file-name-concat save-path "local" "src"))
         (artifact-root (file-name-concat save-path "tool-results" "part one"))
         (local-file (file-name-concat local-root "notes.md"))
         (artifact-file (file-name-concat artifact-root "result.txt"))
         (local-address "local://src/notes.md")
         (artifact-address "artifact://part%20one/result.txt"))
    (unwind-protect
        (progn
          (make-directory local-root t)
          (make-directory artifact-root t)
          (with-temp-file local-file (insert "local note\n"))
          (with-temp-file artifact-file (insert "artifact result\n"))
          (cl-labels
              ((read-resource (address operation path)
                 (let* ((attempt (mevedel-resource-prepare
                                  operation address (list :session session)))
                        (mevedel-resource--current-attempts
                         (list (cons address attempt)))
                        (mevedel--session session)
                        (result (plist-get
                                 (mevedel-tool-fs-read
                                  (list :file_path address))
                                 :result)))
                   (should (string-match-p path result))
                   (should-not (string-match-p
                                (regexp-quote save-path) result))
                   result)))
            (read-resource local-address 'read "local note")
            (read-resource artifact-address 'read "artifact result")
            (let* ((record (mevedel-agent-record--create
                            :path "/root/reviewer" :role "reviewer"
                            :activity 'idle
                            :settled-result "alpha\nbeta\ngamma"
                            :settled-outcome 'completed))
                   (virtual-address "agent://root/reviewer")
                   virtual-attempt)
              (mevedel-session--set-agent-registry
               session (list (cons "/root/reviewer" record)))
              (setq virtual-attempt
                    (mevedel-resource-prepare
                     'read virtual-address (list :session session)))
              (let ((mevedel-resource--current-attempts
                     (list (cons virtual-address virtual-attempt))))
                (let ((result
                       (plist-get
                        (mevedel-tool-fs-read
                         (list :file_path virtual-address :offset 2 :limit 1))
                        :result)))
                  (should (string-match-p "2\11beta" result))
                  (should-not (string-match-p "alpha" result))
                  (should-not (string-match-p (regexp-quote save-path)
                                              result)))))))
      (delete-directory save-path t)))
  :doc "rejects binary and media reads through memory addresses"
  (let* ((workspace-root (make-temp-file "mevedel-memory-media-" t))
         (memory-root (file-name-concat workspace-root "memory"))
         (workspace (mevedel-workspace--create
                     :type 'test :id workspace-root :root workspace-root
                     :name "memory-media"))
         (session (mevedel-session--create
                   :workspace workspace :working-directory workspace-root))
         (mevedel-memory-dirs (list memory-root)))
    (unwind-protect
        (progn
          (make-directory memory-root t)
          (with-temp-file (file-name-concat memory-root "picture.png")
            (insert "not really an image"))
          (let* ((root (car (mevedel-system--memory-roots workspace)))
                 (address (format "memory://%s/picture.png"
                                  (mevedel-resource-memory-root-key root)))
                 (attempt (mevedel-resource-prepare
                           'read address (list :session session)))
                 (mevedel-resource--current-attempts
                  (list (cons address attempt)))
                 (mevedel--session session)
                 (err (should-error
                       (mevedel-tool-fs-read (list :file_path address)))))
            (should (string-match-p "Memory resources only support text reads"
                                    (error-message-string err)))))
      (delete-directory workspace-root t)))
  :doc "successful Read queues changed deeper workspace instructions once per conversation"
  (mevedel-view-test--with-buffers
    (let* ((root (file-name-as-directory
                  (make-temp-file "mevedel-read-instructions-" t)))
           (nested (file-name-concat root "lib"))
           (deeper (file-name-concat nested "private"))
           (sibling (file-name-concat nested "other"))
           (file (file-name-concat deeper "source.el"))
           (sibling-file (file-name-concat sibling "source2.el"))
           (workspace (mevedel-workspace-get-or-create
                       'project root root "read-instructions"))
           (session (mevedel-session-create "main" workspace root))
           (request (mevedel-request--create))
           (draft "> quoted\nsecond line")
           (backend (gptel-make-openai
                     "read-instructions" :key "test" :host "example.test"
                     :models '(test)))
           (data (list :messages [(:role "user" :content "task")]))
           (fsm (gptel-make-fsm
                 :info (list :buffer data-buf :backend backend :data data))))
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
              (mevedel-view-test--insert-composer-draft draft 4))
            (with-current-buffer data-buf
              (setq-local mevedel--session session)
              (setq-local mevedel--workspace workspace)
              (setq-local mevedel--current-request request)
              (mevedel-tool-fs-read (list :file_path file))
              (let* ((bodies
                      (cl-loop for entry in (plist-get
                                             mevedel-reminders--turn-events
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
                (mevedel-tool-fs-read (list :file_path sibling-file))
                (should (= 2 (cl-count-if
                              (lambda (entry)
                                (eq (car-safe (car entry))
                                    'workspace-instructions))
                              (plist-get mevedel-reminders--turn-events
                                         :items)))))
              (should-not
               (mevedel-session-workspace-instruction-hashes session))
              ;; An abort before injection must not suppress the next delivery.
              (setq-local mevedel-reminders--turn-events nil)
              (mevedel-tool-fs-read (list :file_path file))
              (should mevedel-reminders--turn-events)
              (mevedel-reminders--handle-inject fsm)
              (should (mevedel-session-workspace-instruction-hashes session))
              (mevedel-tool-fs-read (list :file_path file))
              (should-not mevedel-reminders--turn-events)
              (write-region "shared changed" nil
                            (file-name-concat nested "AGENTS.md") nil 'silent)
              (mevedel-tool-fs-read (list :file_path file))
              (should
               (string-search
                "shared changed"
                (plist-get
                 (cdr (cl-find-if
                       (lambda (entry)
                         (eq (caar entry) 'workspace-instructions))
                       (plist-get mevedel-reminders--turn-events :items)))
                 :body)))
              (setq-local mevedel-reminders--turn-events nil)
              (setq-local mevedel--agent-invocation
                          (mevedel-agent-invocation--create
                           :path "/root/worker"))
              (mevedel-tool-fs-read (list :file_path file))
              (should (cl-find-if
                       (lambda (entry)
                         (eq (caar entry) 'workspace-instructions))
                       (plist-get mevedel-reminders--turn-events :items))))
            (with-current-buffer view-buf
              (should (string= draft (mevedel-view--input-text)))
              (should (= (point) (+ (mevedel-view--input-start) 4)))))
        (delete-directory root t)))))

(mevedel-deftest mevedel-tool-fs-read--file (:quiet t)
  ,test
  (test)
  :doc "reads full file with line numbers"
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "line one\nline two\nline three\n"))
          (let ((result (mevedel-tool-fs-read--file (list :file_path tmp))))
            (should (string-match-p "1\tline one" result))
            (should (string-match-p "2\tline two" result))
            (should (string-match-p "3\tline three" result))))
      (delete-file tmp)))
  :doc "reads with offset"
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "a\nb\nc\nd\ne\n"))
          (let ((result (mevedel-tool-fs-read--file
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
          (let ((result (mevedel-tool-fs-read--file
                         (list :file_path tmp :offset 1 :limit 2))))
            (should (string-match-p "1\ta" result))
            (should (string-match-p "2\tb" result))
            (should-not (string-match-p "3\tc" result))))
      (delete-file tmp)))
  :doc "rejects a negative text range"
  ;; A negative limit read as an empty success, and a negative offset
  ;; was clamped to line 1 -- content the label does not describe.
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "a\nb\nc\n"))
          (should-error
           (mevedel-tool-fs-read--file (list :file_path tmp :limit -5))
           :type 'error)
          (should-error
           (mevedel-tool-fs-read--file (list :file_path tmp :offset -3))
           :type 'error)
          ;; A negative float must not slip past an integer-only check.
          (should-error
           (mevedel-tool-fs-read--file (list :file_path tmp :offset -3.0))
           :type 'error))
      (delete-file tmp)))
  :doc "reports a range that starts after the last line"
  ;; An empty success is indistinguishable from an empty file, and this
  ;; tool explicitly annotates the genuinely-empty case.  Line four of a
  ;; three-line file is the exact offset a stale continuation hint hands
  ;; the model.
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "a\nb\nc\n"))
          (should-error
           (mevedel-tool-fs-read--file (list :file_path tmp :offset 99))
           :type 'error)
          (should-error
           (mevedel-tool-fs-read--file (list :file_path tmp :offset 4))
           :type 'error)
          ;; The last real line still reads.
          (should (string-match-p
                   "3\tc"
                   (mevedel-tool-fs-read--file
                    (list :file_path tmp :offset 3)))))
      (delete-file tmp)))
  :doc "errors on non-existent file"
  (should-error
   (mevedel-tool-fs-read--file (list :file_path "/nonexistent/file.txt"))
   :type 'error)
  :doc "errors on directory"
  (should-error
   (mevedel-tool-fs-read--file (list :file_path "/tmp"))
   :type 'error)
  :doc "errors on binary extension"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".zip")))
    (unwind-protect
        (should-error
         (mevedel-tool-fs-read--file (list :file_path tmp))
         :type 'error)
      (delete-file tmp)))
  :doc "reads supported image media with base64 envelope when media-capable"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".png")))
    (unwind-protect
        (progn
          (test-mevedel-tool-fs-read--write-bytes
           tmp test-mevedel-tool-fs-read--png-bytes)
          (cl-letf (((symbol-function 'gptel--model-capable-p)
                     (lambda (cap &optional _model) (eq cap 'media)))
                    ((symbol-function 'gptel--model-mime-capable-p)
                     (lambda (mime &optional _model)
                       (equal mime "image/png")))
                    )
            (let ((result (mevedel-tool-fs-read--file (list :file_path tmp))))
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
          (test-mevedel-tool-fs-read--write-bytes
           tmp test-mevedel-tool-fs-read--png-bytes)
          (cl-letf (((symbol-function 'gptel--model-capable-p)
                     (lambda (cap &optional _model) (eq cap 'media)))
                    ((symbol-function 'gptel--model-mime-capable-p)
                     (lambda (mime &optional _model)
                       (equal mime "image/png")))
                    )
            (let ((result (mevedel-tool-fs-read--file
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
          (test-mevedel-tool-fs-read--write-bytes
           tmp test-mevedel-tool-fs-read--png-bytes)
          (cl-letf (((symbol-function 'gptel--model-capable-p)
                     (lambda (cap &optional _model) (eq cap 'media)))
                    ((symbol-function 'gptel--model-mime-capable-p)
                     (lambda (mime &optional _model)
                       (equal mime "image/png"))))
            (let ((err (should-error
                        (mevedel-tool-fs-read--file
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
                      (mevedel-tool-fs-read--file (list :file_path tmp))
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
                      (mevedel-tool-fs-read--file (list :file_path tmp))
                      :type 'error)))
            (should (string-match-p "does not support media type application/pdf"
                                    (cadr err)))))
      (delete-file tmp)))
  :doc "adds bounded-page reminder when reading a large PDF without pages"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".pdf" "%PDF-1.4\n")))
    (unwind-protect
        (cl-letf (((symbol-function 'gptel--model-capable-p)
                   (lambda (cap &optional _model) (eq cap 'media)))
                  ((symbol-function 'gptel--model-mime-capable-p)
                   (lambda (_mime &optional _model) t))

                  ((symbol-function 'mevedel-tool-fs-read-large-pdf-p)
                   (lambda (_path) t))
                  ((symbol-function 'mevedel-tool-fs-read--pdf-page-count)
                   (lambda (_path) 42))
                  ((symbol-function 'mevedel-tool-fs-read--base64-file)
                   (lambda (&rest _) "JVBERg==")))
          (let* ((result (mevedel-tool-fs-read--file (list :file_path tmp)))
                 (body (plist-get result :result)))
            (should (listp result))
            (should (plist-get result :media))
            (should (string-match-p "<system-reminder>" body))
            (should (string-match-p "pages=\"START-END\"" body))
            (should (string-match-p "42 pages" body))))
      (delete-file tmp)))
  :doc "downloads a remote PDF only once across attachment and guidance"
  (let* ((root (file-name-as-directory
                (make-temp-file "mevedel-read-remote-pdf-" t)))
         (local (file-name-concat root "document.pdf"))
         (remote (format "/mevedelmock:pdf-once:%s" local))
         (copy-file-function (symbol-function 'copy-file))
         (copies 0))
    (unwind-protect
        (progn
          (write-region "%PDF-1.4\npayload" nil local nil 'silent)
          (mevedel-test--with-local-shell-tramp '("pdf-once")
            (cl-letf (((symbol-function 'gptel--model-capable-p)
                       (lambda (cap &optional _model) (eq cap 'media)))
                      ((symbol-function 'gptel--model-mime-capable-p)
                       (lambda (_mime &optional _model) t))

                      ((symbol-function 'executable-find)
                       (lambda (name &optional _remote)
                         (and (equal name "pdfinfo") name)))
                      ((symbol-function 'mevedel-execution-run-helper)
                       (lambda (&rest _) '(:exit-code 0 :output "Pages: 42\n")))
                      ((symbol-function 'copy-file)
                       (lambda (file newname &optional ok-if-exists
                                     keep-time preserve-uid-gid
                                     preserve-extended-attributes)
                         (when (file-remote-p file) (cl-incf copies))
                         (funcall copy-file-function file newname ok-if-exists
                                  keep-time preserve-uid-gid
                                  preserve-extended-attributes))))
              (let ((result
                     (mevedel-tool-fs-read--file (list :file_path remote))))
                (should (string-match-p "42 pages"
                                        (plist-get result :result)))
                (should (= 1 copies))))))
      (delete-directory root t)))
  :doc "adds bounded-page reminder when direct PDF attachment is too large"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".pdf" "%PDF-1.4\n")))
    (unwind-protect
        (let ((mevedel-tool-fs-read-media-max-bytes 1))
          (cl-letf (((symbol-function 'gptel--model-capable-p)
                     (lambda (cap &optional _model) (eq cap 'media)))
                    ((symbol-function 'gptel--model-mime-capable-p)
                     (lambda (_mime &optional _model) t))
                    ((symbol-function 'mevedel-tool-fs-read--pdf-page-count)
                     (lambda (_path) 42)))
            (let ((err (should-error
                        (mevedel-tool-fs-read--file
                         (list :file_path tmp))
                        :type 'error)))
              (should (string-match-p "Media file is too large"
                                      (cadr err)))
              (should (string-match-p "<system-reminder>"
                                      (cadr err)))
              (should (string-match-p "pages=\"START-END\""
                                      (cadr err))))))
      (delete-file tmp)))
  :doc "falls back to textual media envelope when backend cannot serialize media"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".png")))
    (unwind-protect
        (progn
          (test-mevedel-tool-fs-read--write-bytes
           tmp test-mevedel-tool-fs-read--png-bytes)
          (cl-letf (((symbol-function 'gptel--model-capable-p)
                     (lambda (cap &optional _model) (eq cap 'media)))
                    ((symbol-function 'gptel--model-mime-capable-p)
                     (lambda (_mime &optional _model) t))
                    )
            (let ((result (mevedel-tool-fs-read--file (list :file_path tmp))))
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
          (test-mevedel-tool-fs-read--write-bytes
           tmp (unibyte-string ?R ?I ?F ?F 0 0 0 0 ?W ?E ?B ?P))
          (test-mevedel-tool-fs-read--write-bytes
           prepared (unibyte-string #xff #xd8 #xff))
          (cl-letf (((symbol-function 'gptel--model-capable-p)
                     (lambda (cap &optional _model) (eq cap 'media)))
                    ((symbol-function 'gptel--model-mime-capable-p)
                     (lambda (mime &optional _model)
                       (equal mime "image/jpeg")))

                    ((symbol-function 'mevedel-tool-fs-read--maybe-transform-media)
                     (lambda (_path _args)
                       (cons prepared "image/jpeg"))))
            (let ((result (mevedel-tool-fs-read--file
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
                  )
          (let ((err (should-error
                      (mevedel-tool-fs-read--file (list :file_path tmp))
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
                      (mevedel-tool-fs-read--file
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

                    ((symbol-function 'executable-find)
                     (lambda (cmd)
                       (and (not (equal cmd "pdftoppm"))
                            (funcall orig-executable-find cmd)))))
            (let ((err (should-error
                        (mevedel-tool-fs-read--file
                         (list :file_path tmp :pages "1"))
                        :type 'error)))
              (should (string-match-p "poppler-utils" (cadr err))))))
      (delete-file tmp)))
  :doc "caps aggregate base64 payload for rendered PDF pages"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".pdf" "%PDF-1.4\n")))
    (unwind-protect
        (let ((mevedel-tool-fs-read--pdf-pages-max-base64-chars 8))
          (cl-letf (((symbol-function 'executable-find)
                     (lambda (cmd) (and (equal cmd "pdftoppm") t)))
                    ((symbol-function 'mevedel-execution-run-helper)
                     (lambda (_name command _read-paths _writable-roots
                                    &rest _)
                       (if (equal (car command) "pdftoppm")
                           (progn
                             (test-mevedel-tool-fs-read--write-bytes
                              (concat (car (last command)) ".png")
                              test-mevedel-tool-fs-read--png-bytes)
                             '(:exit-code 0 :output ""))
                         '(:exit-code 1 :output ""))))
                    ((symbol-function 'gptel--model-capable-p)
                     (lambda (cap &optional _model) (eq cap 'media)))
                    ((symbol-function 'gptel--model-mime-capable-p)
                     (lambda (_mime &optional _model) t))

                    ((symbol-function 'mevedel-tool-fs-read--base64-file)
                     (lambda (&rest _) "aaaaaaaaa")))
            (let ((err (should-error
                        (mevedel-tool-fs-read--file
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
                      (mevedel-tool-fs-read--file
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
                      (mevedel-tool-fs-read--file
                       (list :file_path missing))
                      :type 'error)))
            (should (string-match-p "Did you mean" (cadr err)))
            (should (string-match-p (regexp-quote actual) (cadr err)))))
      (delete-directory parent t)))
  :doc "errors on blocked device path"
  (should-error
   (mevedel-tool-fs-read--file (list :file_path "/dev/zero"))
   :type 'error)
  :doc "returns system-reminder for empty file"
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (let ((result (mevedel-tool-fs-read--file (list :file_path tmp))))
          (should (string-match-p "system-reminder" result))
          (should (string-match-p "empty" result)))
      (delete-file tmp)))
  :doc "truncates long lines"
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert (make-string 3000 ?x) "\nshort\n"))
          (let ((result (mevedel-tool-fs-read--file (list :file_path tmp))))
            (should (string-match-p "\\[\\.\\.\\.]" result))
            (should (string-match-p "2\tshort" result))))
      (delete-file tmp)))
  :doc "caps default text reads with continuation guidance"
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (let ((mevedel-tool-fs-read--default-limit 3)
              (mevedel-tool-fs-read--max-output-chars 10000))
          (with-temp-file tmp
            (dotimes (i 5)
              (insert (format "line %d\n" (1+ i)))))
          (let ((result (mevedel-tool-fs-read--file (list :file_path tmp))))
            (should (string-match-p "1\tline 1" result))
            (should (string-match-p "3\tline 3" result))
            (should-not (string-match-p "4\tline 4" result))
            (should (string-match-p "Read output truncated" result))
            (should (string-match-p "offset=4" result))))
      (delete-file tmp)))
  :doc "remote continuation guidance uses the target-native path"
  (let ((root (file-name-as-directory
               (make-temp-file "mevedel-remote-read-root-" t))))
    (unwind-protect
        (progn
          (write-region "one\ntwo\nthree\n" nil
                        (file-name-concat root "notes.txt") nil 'silent)
          (mevedel-test--with-local-shell-tramp '("read")
            (let* ((remote-root (format "/mevedelmock:read:%s" root))
                   (remote-path (file-name-concat remote-root "notes.txt"))
                   (workspace (mevedel-workspace--create
                               :type 'project :id "remote-read"
                               :root remote-root :name "remote-read"
                               :file-cache (mevedel-test-file-cache-create)))
                   (mevedel--session
                    (mevedel-session-create "main" workspace))
                   (mevedel-tool-fs-read--default-limit 2)
                   (result (mevedel-tool-fs-read--file
                            (list :file_path remote-path))))
              (should (string-match-p
                       (regexp-quote (file-name-concat root "notes.txt"))
                       result))
              (should-not (string-match-p "/mevedelmock:" result)))))
      (delete-directory root t)))
  :doc "caps aggregate text read output with continuation guidance"
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (let ((mevedel-tool-fs-read--default-limit 10)
              (mevedel-tool-fs-read--max-output-chars 25))
          (with-temp-file tmp
            (insert "one\ntwo\nthree\nfour\nfive\n"))
          (let ((result (mevedel-tool-fs-read--file (list :file_path tmp))))
            (should (string-match-p "1\tone" result))
            (should-not (string-match-p "5\tfive" result))
            (should (string-match-p "Read output truncated" result))
            (should (string-match-p "offset=4" result))))
      (delete-file tmp)))
  :doc "errors on oversized file without range"
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (insert (make-string (* 600 1024) ?x)))
          (should-error
           (mevedel-tool-fs-read--file (list :file_path tmp))
           :type 'error))
      (delete-file tmp)))
  :doc "reads oversized file with offset/limit"
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (progn
          (with-temp-file tmp
            (dotimes (i 100)
              (insert (format "line %d: %s\n" (1+ i) (make-string 6000 ?x)))))
          (let ((result (mevedel-tool-fs-read--file
                         (list :file_path tmp :offset 50 :limit 5))))
            (should (string-match-p "50\tline 50" result))
            (should (string-match-p "54\tline 54" result))))
      (delete-file tmp)))
  :doc "preserves a UTF-8 character split across input chunks"
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file tmp
              (insert (make-string (- (* 512 1024) 4) ?x)
                      "\nab\u00e9\n")))
          (let ((result (mevedel-tool-fs-read--file
                         (list :file_path tmp :offset 2 :limit 1))))
            (should (string-match-p
                     (regexp-quote "2\tab\u00e9") result))))
      (delete-file tmp)))
  :doc "preserves a UTF-16 surrogate pair split across input chunks"
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-16le-with-signature))
            (write-region
             (concat (make-string (/ (- (* 512 1024) 8) 2) ?x)
                     "\na\U0001f600\n")
             nil tmp nil 'silent))
          (let ((result (mevedel-tool-fs-read--file
                         (list :file_path tmp :offset 2 :limit 1))))
            (should (string-match-p
                     (regexp-quote "2\ta\U0001f600") result))))
      (delete-file tmp)))
  :doc "ignores newline bytes spanning UTF-16 code units"
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-16le-with-signature))
            (write-region "\u0a41\u0100\nsecond\n"
                          nil tmp nil 'silent))
          (let ((result (mevedel-tool-fs-read--file
                         (list :file_path tmp :offset 2 :limit 1))))
            (should (string-match-p "2\tsecond" result))))
      (delete-file tmp)))
  :doc "counts a bare LF after a DOS-coded input chunk"
  (let ((tmp (make-temp-file "mevedel-test-")))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region
             (concat "one\r\n" (make-string (* 512 1024) ?x)
                     "\nthree\r\n")
             nil tmp nil 'silent))
          (let ((result (mevedel-tool-fs-read--file
                         (list :file_path tmp :offset 3 :limit 1))))
            (should (string-match-p "3\tthree" result))))
      (delete-file tmp)))
  :doc "follows symlinks"
  (let ((tmp (make-temp-file "mevedel-test-"))
        (link (make-temp-file "mevedel-link-")))
    (unwind-protect
        (progn
          (with-temp-file tmp (insert "real content\n"))
          (delete-file link)
          (make-symbolic-link tmp link)
          (let ((result (mevedel-tool-fs-read--file (list :file_path link))))
            (should (string-match-p "real content" result))))
      (delete-file tmp)
      (when (file-exists-p link) (delete-file link))))
  :doc "does not re-resolve a path already canonicalized by the pipeline"
  (let ((tmp (make-temp-file "mevedel-test-" nil ".txt" "content\n")))
    (unwind-protect
        (cl-letf (((symbol-function 'file-truename)
                   (lambda (&rest _)
                     (error "Read handler re-resolved its authorized path"))))
          (should (string-match-p
                   "content"
                   (mevedel-tool-fs-read--file (list :file_path tmp)))))
      (delete-file tmp)))
  :doc "records session interaction and workspace cache entry"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".txt" "hello world\n"))
         (ws (mevedel-workspace--create
              :type 'file :id "read-integration"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (mevedel-test-file-cache-create)))
         (session (mevedel-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 5)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (mevedel-tool-fs-read--file (list :file_path tmp))
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
              :type 'file :id "read-media-dedup"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (mevedel-test-file-cache-create)))
         (session (mevedel-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (progn
          (test-mevedel-tool-fs-read--write-bytes
           tmp test-mevedel-tool-fs-read--png-bytes)
          (with-temp-buffer
            (setq-local mevedel--session session)
            (cl-letf (((symbol-function 'gptel--model-capable-p)
                       (lambda (cap &optional _model) (eq cap 'media)))
                      ((symbol-function 'gptel--model-mime-capable-p)
                       (lambda (_mime &optional _model) t))
                      )
              (let ((first (mevedel-tool-fs-read--file
                            (list :file_path tmp))))
                (should (listp first))
                (should (plist-get first :media)))
              (let ((second (mevedel-tool-fs-read--file
                             (list :file_path tmp))))
                (should (string-match-p "unchanged since last read"
                                        second))
                (should-not (string-match-p "<media-file>" second))))))
      (delete-file tmp)))
  :doc "returns stub on duplicate read when mtime unchanged"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".txt" "hello world\n"))
         (ws (mevedel-workspace--create
              :type 'file :id "read-dedup"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (mevedel-test-file-cache-create)))
         (session (mevedel-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (let ((first (mevedel-tool-fs-read--file (list :file_path tmp))))
            (should (string-match-p "hello world" first)))
          (let ((second (mevedel-tool-fs-read--file (list :file_path tmp))))
            (should (string-match-p "unchanged since last read" second))
            (should-not (string-match-p "hello world" second))))
      (delete-file tmp)))
  :doc "sub-agent reads ignore parent-session dedup state"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".txt" "agent content\n"))
         (ws (mevedel-workspace--create
              :type 'file :id "read-agent-dedup"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (mevedel-test-file-cache-create)))
         (session (mevedel-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (let ((first (mevedel-tool-fs-read--file (list :file_path tmp))))
            (should (string-match-p "agent content" first)))
          (let ((second (mevedel-tool-fs-read--file (list :file_path tmp))))
            (should (string-match-p "unchanged since last read" second)))
          (setq-local mevedel--agent-invocation t)
          (let ((agent-read (mevedel-tool-fs-read--file
                             (list :file_path tmp))))
            (should (string-match-p "agent content" agent-read))
            (should-not (string-match-p "unchanged since last read"
                                        agent-read)))
          (setq-local mevedel--agent-invocation nil)
          ;; The parent session still sees its own dedup state; the
          ;; agent read did not replace or clear it.
          (let ((after-agent (mevedel-tool-fs-read--file
                              (list :file_path tmp))))
            (should (string-match-p "unchanged since last read"
                                    after-agent))))
      (delete-file tmp)))
  :doc "does not dedupe after external modification"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".txt" "hello\n"))
         (ws (mevedel-workspace--create
              :type 'file :id "read-dedup-mtime"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (mevedel-test-file-cache-create)))
         (session (mevedel-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (mevedel-tool-fs-read--file (list :file_path tmp))
          (let ((future (time-add (current-time) 2)))
            (with-temp-file tmp (insert "goodbye\n"))
            (set-file-times tmp future))
          (let ((second (mevedel-tool-fs-read--file (list :file_path tmp))))
            (should (string-match-p "goodbye" second))
            (should-not (string-match-p "unchanged since last read" second))))
      (delete-file tmp)))
  :doc "does not dedupe when range differs"
  (let* ((tmp (make-temp-file "mevedel-test-" nil ".txt"
                              "a\nb\nc\nd\ne\nf\n"))
         (ws (mevedel-workspace--create
              :type 'file :id "read-dedup-range"
              :root (file-name-directory tmp)
              :name "test"
              :file-cache (mevedel-test-file-cache-create)))
         (session (mevedel-session--create
                   :name "main" :workspace ws
                   :touched-files (make-hash-table :test #'equal)
                   :turn-count 1)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (mevedel-tool-fs-read--file
           (list :file_path tmp :offset 1 :limit 2))
          (let ((second (mevedel-tool-fs-read--file
                         (list :file_path tmp :offset 4 :limit 2))))
            (should (string-match-p "4\td" second))
            (should-not (string-match-p "unchanged since last read" second))))
      (delete-file tmp)))
  :doc "remote session artifacts ignore poisoned fixed caches and file-state dedup"
  (let* ((host "read-artifact")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-read-artifact-" t)))
         (remote-root (format "/mevedelmock:%s:%s/" host local-root))
         (workspace (mevedel-workspace--create
                     :type 'project :id "read-artifact"
                     :root remote-root :name "read-artifact"))
         (session (mevedel-session-create "main" workspace))
         (save-path (file-name-concat remote-root ".mevedel" "sessions"
                                     "session"))
         (logical "plans/current.md")
         (fixed (file-name-concat save-path logical))
         (published (file-name-concat
                     save-path ".publications" "generation" "000001.data"))
         (committed "committed plan\nsecond line\n")
         dedup-called)
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (make-directory (file-name-directory fixed) t)
          (write-region "poisoned fixed cache\n" nil fixed nil 'silent)
          (make-directory (file-name-directory published) t)
          (write-region committed nil published nil 'silent)
          (setf (mevedel-session-save-path session)
                (file-name-as-directory save-path)
                (mevedel-session-publication session)
                (list :head ".publications/generation/manifest.el"
                      :sidecar published
                      :artifacts
                      (list (list logical
                                  :published published
                                  :sha256 (secure-hash 'sha256 committed)))))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (cl-letf (((symbol-function 'mevedel-session-read-is-duplicate-p)
                       (lambda (&rest _)
                         (setq dedup-called t)))
                      ((symbol-function 'mevedel-session-record-file-access)
                       (lambda (&rest _)
                         (setq dedup-called t))))
              (let ((result
                     (mevedel-tool-fs-read--file
                      (list :file_path fixed :offset 2 :limit 1))))
                (should (string-match-p "2\tsecond line" result))
                (should-not (string-match-p "poisoned" result))
                (should-not dedup-called)))
            (delete-file fixed)
            (should (string-match-p
                     "1\tcommitted plan"
                     (mevedel-tool-fs-read--file
                      (list :file_path fixed :offset 1 :limit 1))))))
      (when (file-directory-p local-root)
        (delete-directory local-root t))))
  :doc "remote session artifacts never fall back when the manifest omits them"
  (let* ((host "read-unpublished-artifact")
         (local-root (file-name-as-directory
                      (make-temp-file "mevedel-read-unpublished-" t)))
         (remote-root (format "/mevedelmock:%s:%s/" host local-root))
         (workspace (mevedel-workspace--create
                     :type 'project :id "read-unpublished-artifact"
                     :root remote-root :name "read-unpublished-artifact"))
         (session (mevedel-session-create "main" workspace))
         (save-path (file-name-concat remote-root ".mevedel" "sessions"
                                     "session"))
         (fixed (file-name-concat save-path "tool-results" "result.txt")))
    (unwind-protect
        (mevedel-test--with-local-shell-tramp (list host)
          (make-directory (file-name-directory fixed) t)
          (write-region "fixed cache only\n" nil fixed nil 'silent)
          (setf (mevedel-session-save-path session)
                (file-name-as-directory save-path)
                (mevedel-session-publication session)
                (list :head ".publications/generation/manifest.el"
                      :sidecar nil :artifacts nil))
          (with-temp-buffer
            (setq-local mevedel--session session)
            (should-error
             (mevedel-tool-fs-read--file (list :file_path fixed))
             :type 'error)))
      (when (file-directory-p local-root)
        (delete-directory local-root t)))))

;;
;;; Line number formatting

(mevedel-deftest mevedel-tool-fs-read--add-line-numbers ()
  ,test
  (test)
  :doc "adds correct line numbers starting at 1"
  (with-temp-buffer
    (insert "alpha\nbeta\ngamma\n")
    (mevedel-tool-fs-read--add-line-numbers 1)
    (should (string-match-p "1\talpha" (buffer-string)))
    (should (string-match-p "2\tbeta" (buffer-string)))
    (should (string-match-p "3\tgamma" (buffer-string))))
  :doc "adds correct line numbers with offset"
  (with-temp-buffer
    (insert "alpha\nbeta\n")
    (mevedel-tool-fs-read--add-line-numbers 42)
    (should (string-match-p "42\talpha" (buffer-string)))
    (should (string-match-p "43\tbeta" (buffer-string))))
  :doc "truncates lines over 2000 characters"
  (with-temp-buffer
    (insert (make-string 3000 ?a) "\n")
    (mevedel-tool-fs-read--add-line-numbers 1)
    (should (string-match-p "\\[\\.\\.\\.]" (buffer-string)))
    (should (<= (length (buffer-string)) 2100))))

;;
;;; Directory listing helper

(mevedel-deftest mevedel-tool-fs-read-list-directory ()
  ,test
  (test)
  :doc "lists files in a directory, sorted by path"
  (let ((tmp-dir (make-temp-file "mevedel-list-" t)))
    (unwind-protect
        (progn
          (with-temp-file (file-name-concat tmp-dir "alpha.txt") (insert "a"))
          (with-temp-file (file-name-concat tmp-dir "beta.txt") (insert "b"))
          (let* ((result (mevedel-tool-fs-read-list-directory tmp-dir))
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
          (let* ((result (mevedel-tool-fs-read-list-directory tmp-dir 3))
                 (entries (car result))
                 (truncated (cdr result)))
            (should (= 3 (length entries)))
            (should truncated)))
      (delete-directory tmp-dir t)))

  :doc "does not traverse descendant symlinks outside the selected root"
  (let* ((base (file-name-as-directory
                (make-temp-file "mevedel-list-symlink-" t)))
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
          (let ((entries (car (mevedel-tool-fs-read-list-directory root))))
            (should (member "inside.txt" entries))
            (should-not (member "linked/secret.txt" entries))))
      (delete-directory base t)))

  :doc "returns empty list for empty directory"
  (let ((tmp-dir (make-temp-file "mevedel-list-" t)))
    (unwind-protect
        (let ((result (mevedel-tool-fs-read-list-directory tmp-dir)))
          (should (null (car result)))
          (should (null (cdr result))))
      (delete-directory tmp-dir t)))

  :doc "errors on non-directory path"
  (let ((tmp (make-temp-file "mevedel-list-" nil ".txt" "x")))
    (unwind-protect
        (should-error (mevedel-tool-fs-read-list-directory tmp) :type 'error)
      (delete-file tmp))))

(mevedel-deftest mevedel-tool-fs-read-render ()
  ,test
  (test)
  :doc "returns nil for non-string result"
  (should (null (mevedel-tool-fs-read-render "Read" '(:file_path "/x.el") nil nil)))

  :doc "returns nil for error results"
  (should (null
           (mevedel-tool-fs-read-render
            "Read" '(:file_path "/missing.el")
            "Error: File /missing.el does not exist" nil)))

  :doc "header shows basename and line count; body-mode from extension"
  (let* ((body "1->foo\n2->bar\n3->baz\n")
         (plist (mevedel-tool-fs-read-render
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
            (let ((plist (mevedel-tool-fs-read-render
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
            (let ((plist (mevedel-tool-fs-read-render
                          "Read" `(:file_path ,file) "1->subdir\n" nil)))
              (should (string-match-p
                       "\\`Read: test/test-mevedel-skills\\.el "
                       (plist-get plist :header))))))
      (delete-directory root t)))

  :doc "body-mode is nil when file has no recognized extension"
  (let* ((plist (mevedel-tool-fs-read-render
                 "Read" '(:file_path "/tmp/no-extension-here") "x\n" nil)))
    (should (null (plist-get plist :body-mode))))

  :doc "body-mode is nil for media files"
  (let* ((plist (mevedel-tool-fs-read-render
                 "Read" '(:file_path "/tmp/screenshot.png")
                 "<media-file>\n...\n</media-file>" nil)))
    (should (null (plist-get plist :body-mode))))

  :doc "header line count ignores appended system reminders"
  (let* ((body "1->foo\n2->bar\n\n<system-reminder>\nuse Imenu\n</system-reminder>")
         (plist (mevedel-tool-fs-read-render
                 "Read" '(:file_path "/tmp/a.el") body nil)))
    (should (string-match-p "2 lines" (plist-get plist :header))))

  :doc "header line count preserves literal system reminder content"
  (let* ((body "1->before\n2-><system-reminder>\n3->sample\n4-></system-reminder>\n5->after")
         (plist (mevedel-tool-fs-read-render
                 "Read" '(:file_path "/tmp/a.el") body nil)))
    (should (string-match-p "5 lines" (plist-get plist :header)))))

(provide 'test-mevedel-tool-fs-read)
;;; test-mevedel-tool-fs-read.el ends here
