;;; test-mevedel-tool-fs-read-long-lines.el --- Character paging tests -*- lexical-binding: t -*-

;;; Commentary:

;; Follow long-line continuations through the registered tool pipeline.

;;; Code:

(require 'cl-lib)
(require 'mevedel-tools)
(require 'mevedel-tool-fs-read)
(require 'mevedel-tool-fs-search)
(require 'mevedel-file-state)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defun test-mevedel-read-long-lines--call (name args)
  "Call registered tool NAME with ARGS and await its model-visible text."
  (let (done result)
    (mevedel-pipeline-run-tool
     (mevedel-tool-get name)
     (lambda (value) (setq result value done t)) args)
    (with-timeout (20 (ert-fail "Tool did not settle"))
      (while (not done) (accept-process-output nil 0.01)))
    (should (stringp result))
    result))

(defun test-mevedel-read-long-lines--retry (text)
  "Extract the exact character continuation arguments from TEXT, or nil."
  (when (string-match
         "Read(file_path=\\(\".*\"\\), offset=\\([0-9]+\\), limit=1, char_offset=\\([0-9]+\\)) to continue this line"
         text)
    (list :file_path (car (read-from-string (match-string 1 text)))
          :offset (string-to-number (match-string 2 text))
          :limit 1 :char_offset (string-to-number (match-string 3 text)))))

(mevedel-deftest mevedel-tool-fs-read-character-pages ()
  ,test
  (test)
  :doc "registered Read reconstructs a 7849-character line for files, artifacts and live history"
  (let* ((base (make-temp-file "mevedel-long-lines-" t))
         (save (file-name-concat base "session"))
         (file (file-name-concat base "escaped\nlog.txt"))
         (artifact (file-name-concat save "tool-results" "executions" "escaped log.log"))
         (workspace (mevedel-workspace--create
                     :type 'test :id "long-lines" :root base :name "long-lines"
                     :file-cache (mevedel-test-file-cache-create)))
         (session (mevedel-session--create
                   :authority-mode 'pid-lock :save-path save
                   :touched-files (make-hash-table :test #'equal)
                   :working-directory base :workspace workspace
                   :permission-mode 'full-auto))
         ;; Multibyte characters and literal escape sequences distinguish
         ;; character offsets from byte offsets and unescaped backtrace text.
         (mevedel-permission-mode 'full-auto)
         (line (concat (make-string 1999 ?a) "\u00e4"
                       (make-string 1999 ?b) "\u03bb"
                       (make-string 1999 ?c) "\\n"
                       (make-string 1840 ?d) "DECISIVE")))
    (unwind-protect
        (with-temp-buffer
          (org-mode)
          (setq-local mevedel--session session)
          (mevedel-session-set-root-buffer session (current-buffer))
          (insert "heading\n" line "\nlast\n")
          (make-directory (file-name-directory artifact) t)
          (dolist (path (list file artifact))
            (with-temp-file path (insert "heading\n" line "\nlast\n")))
          (mevedel-tool-fs--register)
          (should (= 7849 (length line)))
          (dolist (address (list file "artifact://executions/escaped%20log.log" "history://root"))
            (let* ((grep (test-mevedel-read-long-lines--call
                          "Grep" (list :path address :pattern "DECISIVE"
                                       :output_mode "content")))
                   ;; History adds a role heading. Discover the line from the
                   ;; same projection the caller searches, not raw Org offsets.
                   (line-number (if (string-prefix-p "history://" address) 3 2))
                   (args (list :file_path address :offset line-number :limit 1))
                   (reconstructed "") (pages 0))
              (should-not (string-prefix-p "Error:" grep))
              (should (string-search (format "%d:" line-number) grep))
              (while args
                (cl-incf pages)
                (should (<= pages 4))
                (let* ((result (test-mevedel-read-long-lines--call "Read" args))
                       (retry (test-mevedel-read-long-lines--retry result)))
                  (should-not (string-prefix-p "Error:" result))
                  (should (<= (length result) mevedel-tool-fs-read--max-output-chars))
                  (when (not (equal address file))
                    (should-not (string-search base result)))
                  (should (string-match "\\`[ ]*[0-9]+\t" result))
                  (let* ((start (match-end 0))
                         (end (if retry (string-search " [... Read(" result start)
                                (or (string-search "\n" result start) (length result)))))
                    (setq reconstructed (concat reconstructed (substring result start end))))
                  (when retry
                    (should (equal address (plist-get retry :file_path)))
                    (should (= line-number (plist-get retry :offset)))
                    (should (= (* pages 2000) (plist-get retry :char_offset))))
                  (setq args retry)))
              (should (= pages 4))
              (should (equal line reconstructed)))))
      (delete-directory base t)))

  :doc "registered Read validates character offsets and isolates duplicate ranges"
  (let* ((base (make-temp-file "mevedel-character-ranges-" t))
         (file (file-name-concat base "line\nfile.txt"))
         (workspace (mevedel-workspace--create
                     :type 'test :id "ranges" :root base :name "ranges"
                     :file-cache (mevedel-test-file-cache-create)))
         (session (mevedel-session--create
                   :touched-files (make-hash-table :test #'equal)
                   :workspace workspace :working-directory base
                   :permission-mode 'full-auto)))
    (unwind-protect
        (with-temp-buffer
          (setq-local mevedel--session session)
          (mevedel-tool-fs--register)
          (with-temp-file file (insert (make-string 2000 ?a) "tail\nnext\n"))
          (cl-labels ((read-page (&rest options)
                       (test-mevedel-read-long-lines--call
                        "Read" (append (list :file_path file :offset 1) options))))
            (should (string-search "char_offset=2000" (read-page :limit 1)))
            (should (string-prefix-p "1\ttail\n" (read-page :char_offset 2000)))
            (should (string-search "unchanged" (read-page :char_offset 2000)))
            (should (string-search "char_offset=2000" (read-page :limit 1)))
            (dolist (options '((:char_offset -1) (:char_offset 1 :limit 2)
                               (:char_offset 3000 :limit 1)))
              (should (string-prefix-p "Error:" (apply #'read-page options))))
            (should (string-prefix-p "1\t\n" (read-page :char_offset 2004)))
            (should (string-search "char_offset=0" (read-page :char_offset 2005)))
            ;; Explicit zero keeps ordinary multiline reads intact.
            (should (string-search "2\tnext" (read-page :char_offset 0 :limit 2)))))
      (delete-directory base t)))

  :doc "character pages reject media and preserve pending status and aggregate caps"
  (let ((base (make-temp-file "mevedel-character-bounds-" t)))
    (unwind-protect
        (with-temp-buffer
          (let* ((session (mevedel-session--create
                           :touched-files (make-hash-table :test #'equal)
                           :permission-mode 'full-auto))
                 (image (file-name-concat base "image.png"))
                 (file (file-name-concat base "many.txt"))
                 (record (mevedel-agent-record--create
                          :path "/root/busy" :activity 'running)))
            (setq-local mevedel--session session)
            (mevedel-session--set-agent-registry session (list (cons "/root/busy" record)))
            (mevedel-tool-fs--register)
            (with-temp-file image (insert "not decoded"))
            (should (string-search "only supported for text"
                                   (test-mevedel-read-long-lines--call
                                    "Read" (list :file_path image :char_offset 1))))
            (should (string-search "NOT READY"
                                   (test-mevedel-read-long-lines--call
                                    "Read" '(:file_path "agent://root/busy"
                                             :char_offset 2000 :limit 1))))
            (with-temp-file file
              (dotimes (_ 100) (insert (make-string 3000 ?x) "\n")))
            (let ((page (test-mevedel-read-long-lines--call
                         "Read" (list :file_path file :limit 100))))
              (should (string-search "Read output truncated" page))
              ;; Existing aggregate cap excludes the final continuation hint.
              (should (< (length page) (+ mevedel-tool-fs-read--max-output-chars 1000))))))
      (delete-directory base t)))

  :doc "registered remote Read keeps buffer-local session retries target-native"
  (let* ((base (file-name-as-directory (make-temp-file "mevedel-remote-chars-" t)))
         (file (file-name-concat base "line.txt"))
         (mevedel-permission-mode 'full-auto))
    (unwind-protect
        (progn
          (with-temp-file file (insert (make-string 2000 ?x) "tail\n"))
          (mevedel-test--with-local-shell-tramp '("character-page")
            (let* ((remote-root (format "/mevedelmock:character-page:%s" base))
                   (workspace (mevedel-workspace--create
                               :type 'test :id "remote-chars" :root remote-root
                               :name "remote-chars"
                               :file-cache (mevedel-test-file-cache-create)))
                   (session (mevedel-session-create "main" workspace)))
              (setf (mevedel-execution-target-environment
                     (mevedel-session-execution-target session))
                    (list (cons "HOME" base)))
              (with-temp-buffer
                ;; A dynamic session binding would survive the temporary
                ;; buffer boundary and mask the missing visible-path capture.
                (setq-local mevedel--session session)
                (mevedel-tool-fs--register)
                (dolist (range '((nil nil) (1 1)))
                  (let* ((page (apply #'mevedel-tool-fs-read-slurp-file-contents
                                      (file-name-concat remote-root "line.txt") range))
                         (retry (test-mevedel-read-long-lines--retry page)))
                    (should (equal file (plist-get retry :file_path)))))
                (let* ((page (test-mevedel-read-long-lines--call
                              "Read" (list :file_path file :offset 1 :limit 1)))
                       (retry (test-mevedel-read-long-lines--retry page)))
                  (should-not (string-prefix-p "Error:" page))
                  (should retry)
                  (should (equal file (plist-get retry :file_path)))
                  (should-not (string-search "/mevedelmock:" page))
                  (should (string-prefix-p
                           "1\ttail\n"
                           (test-mevedel-read-long-lines--call "Read" retry))))))))
      (delete-directory base t))))

(provide 'test-mevedel-tool-fs-read-long-lines)
;;; test-mevedel-tool-fs-read-long-lines.el ends here
