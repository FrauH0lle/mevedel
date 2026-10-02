;;; test-mevedel-collaboration-files.el --- Browser project file tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests browsing, reading, uploading and removing project files from
;; browser guests: the project listing as the authority, folder and file
;; resolution, tier checks, chunked transfer both ways, and trashing.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'cl-lib)
(require 'mevedel-collaboration-files)
(require 'mevedel-collaboration)
(require 'mevedel-collaboration-guest)
(require 'mevedel-collaboration-transport)

(defmacro mevedel-collaboration-files-test--with-root (root &rest body)
  "Bind ROOT to a fresh temporary project directory around BODY."
  (declare (indent 1))
  `(let ((,root (file-name-as-directory
                 (make-temp-file "mevedel-files-" t))))
     (unwind-protect (progn ,@body)
       (delete-directory ,root t))))

(defun mevedel-collaboration-files-test--write (root path &optional content)
  "Write CONTENT, default PATH itself, to PATH under ROOT."
  (let ((file (file-name-concat root path)))
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'binary))
      (write-region (or content path) nil file nil 'silent))
    file))

(defun mevedel-collaboration-files-test--owner ()
  "Return a lobby-like owner with a viewer (1) and a full guest (2)."
  (let ((guests (make-hash-table :test #'eql)))
    (puthash 1 (list :name "viewer" :writable nil) guests)
    (puthash 2 (list :name "writer" :writable t) guests)
    (list :transport 'transport :guests guests)))

(defmacro mevedel-collaboration-files-test--capturing (sent &rest body)
  "Run BODY with sent frames pushed onto SENT as (PEER . FRAME)."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'mevedel-collaboration--transport-send)
              (lambda (_transport peer frame)
                (push (cons peer frame) ,sent)
                t)))
     ,@body))

(mevedel-deftest mevedel-collaboration-files--listing
  (:doc "lists project files without workspace state, following ignore rules")
  (progn
    (mevedel-collaboration-files-test--with-root root
      (mevedel-collaboration-files-test--write root "notes.md")
      (mevedel-collaboration-files-test--write root "data/table.csv")
      (mevedel-collaboration-files-test--write root ".mevedel/lobby")
      (mevedel-collaboration-files-test--write root ".mevedel/shared/x.md")
      (should (equal '("data/table.csv" "notes.md")
                     (mevedel-collaboration-files--listing root))))
    (mevedel-collaboration-files-test--with-root root
      (let ((default-directory root))
        (should (zerop (call-process "git" nil nil nil "init" "--quiet"))))
      (mevedel-collaboration-files-test--write root ".gitignore" "*.log\n")
      (mevedel-collaboration-files-test--write root "src/main.py")
      (mevedel-collaboration-files-test--write root "debug.log")
      (mevedel-collaboration-files-test--write root ".mevedel/shared/x.md")
      (should (equal '(".gitignore" "src/main.py")
                     (mevedel-collaboration-files--listing root)))
      ;; A user's own project backend, possibly cached, is not consulted.
      (let ((project-find-functions
             (list (lambda (_dir) (cons 'transient "/nowhere/")))))
        (should (equal '(".gitignore" "src/main.py")
                       (mevedel-collaboration-files--listing root)))))))

(mevedel-deftest mevedel-collaboration-files--folder-p
  (:doc "accepts the root and folders that hold listed files only")
  (let ((listing '("a/b/c.txt" "ab.txt")))
    (should (mevedel-collaboration-files--folder-p listing ""))
    (should (mevedel-collaboration-files--folder-p listing "a"))
    (should (mevedel-collaboration-files--folder-p listing "a/b"))
    (should-not (mevedel-collaboration-files--folder-p listing "a/"))
    (should-not (mevedel-collaboration-files--folder-p listing "ab.txt"))
    (should-not (mevedel-collaboration-files--folder-p listing "../a"))
    (should-not (mevedel-collaboration-files--folder-p listing nil))))

(mevedel-deftest mevedel-collaboration-files--entries
  (:doc "lists one folder's children, folders first, skipping stale names")
  (mevedel-collaboration-files-test--with-root root
    (mevedel-collaboration-files-test--write root "a/x.txt")
    (mevedel-collaboration-files-test--write root "a/y/z.txt")
    (mevedel-collaboration-files-test--write root "b.txt" "four")
    (mevedel-collaboration-files-test--write root "outside")
    (make-symbolic-link (file-name-concat root "outside")
                        (file-name-concat root "link.txt"))
    (let ((listing '("a/x.txt" "a/y/z.txt" "b.txt" "gone.txt" "link.txt")))
      (should (equal '(((:name "a" :kind "dir")
                        (:name "b.txt" :kind "file" :size 4))
                       . 0)
                     (mevedel-collaboration-files--entries root listing "")))
      (should (equal '(((:name "y" :kind "dir")
                        (:name "x.txt" :kind "file" :size 7))
                       . 0)
                     (mevedel-collaboration-files--entries root listing "a")))
      (let ((mevedel-collaboration-files--max-entries 1))
        (should (equal 1 (cdr (mevedel-collaboration-files--entries
                               root listing ""))))))))

(mevedel-deftest mevedel-collaboration-files--file
  (:doc "resolves listed plain files and refuses everything else")
  (mevedel-collaboration-files-test--with-root root
    (let ((file (mevedel-collaboration-files-test--write root "a.txt")))
      (mevedel-collaboration-files-test--write root "private.txt")
      (make-symbolic-link file (file-name-concat root "link.txt"))
      (let ((listing '("a.txt" "link.txt" "gone.txt")))
        (should (equal file (mevedel-collaboration-files--file
                             root listing "a.txt")))
        (dolist (path '("private.txt" "link.txt" "gone.txt" "../a.txt" nil))
          (should-error (mevedel-collaboration-files--file
                         root listing path)))))))

(mevedel-deftest mevedel-collaboration-files--name-p
  (:doc "accepts one visible path component only")
  (progn
    (should (mevedel-collaboration-files--name-p "report 2.pdf"))
    (should (mevedel-collaboration-files--name-p "notes.md"))
    (dolist (name '("" "a/b" "a\\b" ".env" "~root" "tab\tname" nil))
      (should-not (mevedel-collaboration-files--name-p name)))
    (should-not (mevedel-collaboration-files--name-p (make-string 256 ?x)))))

(mevedel-deftest mevedel-collaboration-files--parent
  (:doc "names a path's folder, the root as the empty string")
  (progn
    (should (equal "" (mevedel-collaboration-files--parent "a.txt")))
    (should (equal "a/b" (mevedel-collaboration-files--parent "a/b/c.txt")))))

(mevedel-deftest mevedel-collaboration-files--reply
  (:doc "addresses a typed answer to one peer")
  (let (sent)
    (mevedel-collaboration-files-test--capturing sent
      (mevedel-collaboration-files--reply
       '(:transport transport) 4 "files" 9 '(:ok t)))
    (should (equal '((4 :t "files" :reqId 9 :ok t)) sent))))

(mevedel-deftest mevedel-collaboration-files--announce
  (:doc "tells writable guests only")
  (let ((owner (mevedel-collaboration-files-test--owner))
        sent)
    (mevedel-collaboration-files-test--capturing sent
      (mevedel-collaboration-files--announce owner "docs"))
    (should (equal '((2 :t "files-changed" :dir "docs")) sent))))

(mevedel-deftest mevedel-collaboration-files--mime
  (:doc "types known files and previews untyped UTF-8 text as plain text")
  (progn
    (should (equal "text/markdown"
                   (mevedel-collaboration-files--mime "a.md" "# x")))
    (should (equal "text/plain"
                   (mevedel-collaboration-files--mime "a.el" "(provide 'a)")))
    (should (equal "application/octet-stream"
                   (mevedel-collaboration-files--mime "a.bin" "\0\1")))))

(mevedel-deftest mevedel-collaboration-files-handle-list
  (:doc "answers full links with a folder listing and refuses the rest")
  (mevedel-collaboration-files-test--with-root root
    (mevedel-collaboration-files-test--write root "docs/a.md")
    (let ((owner (mevedel-collaboration-files-test--owner))
          sent)
      (mevedel-collaboration-files-test--capturing sent
        (let ((ask (lambda (peer frame)
                     (setq sent nil)
                     (mevedel-collaboration-files-handle-list
                      owner peer frame root)
                     (cdar sent))))
          (should (equal '(:t "files" :reqId 1 :dir ""
                              :entries [(:name "docs" :kind "dir")] :omitted 0)
                         (funcall ask 2 '(:reqId 1 :dir ""))))
          (should (equal '(:t "files" :reqId 2 :dir "docs"
                              :entries [(:name "a.md" :kind "file" :size 9)]
                              :omitted 0)
                         (funcall ask 2 '(:reqId 2 :dir "docs"))))
          (should (equal "This folder is not in the project"
                         (plist-get (funcall ask 2 '(:reqId 3 :dir ".mevedel"))
                                    :error)))
          (should (string-match-p
                   "view link"
                   (plist-get (funcall ask 1 '(:reqId 4 :dir "")) :error)))
          ;; Unknown peers and malformed request ids are not answered.
          (should-not (funcall ask 7 '(:reqId 5 :dir "")))
          (should-not (funcall ask 2 '(:reqId "x" :dir ""))))))))

(mevedel-deftest mevedel-collaboration-files-handle-get
  (:doc "streams listed files in chunks and refuses unlisted ones")
  (mevedel-collaboration-files-test--with-root root
    (let ((content (make-string 2000 ?x))
          (owner (mevedel-collaboration-files-test--owner))
          sent)
      (mevedel-collaboration-files-test--write root "src/main.py" content)
      (mevedel-collaboration-files-test--write root ".mevedel/lobby" "secret")
      (mevedel-collaboration-files-test--capturing sent
        (let ((mevedel-collaboration--max-frame-json-bytes 800))
          (mevedel-collaboration-files-handle-get
           owner 2 '(:reqId 3 :path "src/main.py") root))
        (setq sent (nreverse sent))
        (should (> (length sent) 1))
        (let ((head (cdar sent)))
          (should (equal "file" (plist-get head :t)))
          (should (equal "src/main.py" (plist-get head :path)))
          (should (equal "main.py" (plist-get head :name)))
          (should (equal "text/plain" (plist-get head :mime)))
          (should (= 2000 (plist-get head :size))))
        (should (eq t (plist-get (cdar (last sent)) :final)))
        (should (equal content
                       (base64-decode-string
                        (mapconcat (lambda (entry) (plist-get (cdr entry) :data))
                                   sent))))
        (dolist (case '((2 . ".mevedel/lobby") (2 . "../x") (1 . "src/main.py")))
          (setq sent nil)
          (mevedel-collaboration-files-handle-get
           owner (car case) (list :reqId 4 :path (cdr case)) root)
          (should (= 1 (length sent)))
          (should (stringp (plist-get (cdar sent) :error)))
          (should-not (plist-get (cdar sent) :data)))
        (let ((mevedel-collaboration--max-artifact-bytes 100))
          (setq sent nil)
          (mevedel-collaboration-files-handle-get
           owner 2 '(:reqId 5 :path "src/main.py") root)
          (should (string-match-p "too large"
                                  (plist-get (cdar sent) :error))))))))

(mevedel-deftest mevedel-collaboration-files--begin-upload
  (:doc "admits a free plain name in a listed folder within the bound")
  (mevedel-collaboration-files-test--with-root root
    (mevedel-collaboration-files-test--write root "docs/a.md")
    (should (equal '(:req-id 1 :path "docs/b.md" :size 3 :received 0 :parts nil)
                   (mevedel-collaboration-files--begin-upload
                    root '(:dir "docs" :name "b.md" :size 3) 1)))
    (should (equal "b.md"
                   (plist-get (mevedel-collaboration-files--begin-upload
                               root '(:dir "" :name "b.md" :size 0) 1)
                              :path)))
    ;; A share from a prompt takes the next free name instead.
    (should (equal "docs/a-2.md"
                   (plist-get (mevedel-collaboration-files--begin-upload
                               root '(:dir "docs" :name "a.md" :size 1
                                           :rename t)
                               1)
                              :path)))
    (dolist (frame `((:dir "nowhere" :name "b.md" :size 3)
                     (:dir ".mevedel" :name "b.md" :size 3)
                     (:dir "docs" :name ".env" :size 3)
                     (:dir "docs" :name "a.md" :size 3)
                     (:dir "docs" :name "b.md" :size -1)
                     (:dir "docs" :name "b.md"
                           :size ,(1+ mevedel-collaboration-files--max-upload-bytes))))
      (should-error (mevedel-collaboration-files--begin-upload root frame 1)))))

(mevedel-deftest mevedel-collaboration-files--free-path
  (:doc "keeps a free name and numbers a taken one")
  (mevedel-collaboration-files-test--with-root root
    (mevedel-collaboration-files-test--write root "a.png")
    (mevedel-collaboration-files-test--write root "a-2.png")
    (mevedel-collaboration-files-test--write root "Makefile")
    (should (equal "b.png" (mevedel-collaboration-files--free-path root "b.png")))
    (should (equal "a-3.png" (mevedel-collaboration-files--free-path root "a.png")))
    (should (equal "Makefile-2"
                   (mevedel-collaboration-files--free-path root "Makefile")))))

(mevedel-deftest mevedel-collaboration-files--finish-upload
  (:doc "writes a new file once and refuses short, taken or ignored ones")
  (mevedel-collaboration-files-test--with-root root
    (let ((default-directory root))
      (should (zerop (call-process "git" nil nil nil "init" "--quiet"))))
    (mevedel-collaboration-files-test--write root ".gitignore" "*.log\n")
    (let ((upload (lambda (path parts size)
                    (list :req-id 1 :path path :size size :parts parts))))
      (should (equal "a.bin" (mevedel-collaboration-files--finish-upload
                              root (funcall upload "a.bin" '("\2\3" "\0\1") 4))))
      (should (equal "\0\1\2\3"
                     (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally
                        (file-name-concat root "a.bin"))
                       (buffer-string))))
      (should-error (mevedel-collaboration-files--finish-upload
                     root (funcall upload "a.bin" '("x") 1)))
      (should-error (mevedel-collaboration-files--finish-upload
                     root (funcall upload "b.bin" '("x") 2)))
      (should-not (file-exists-p (file-name-concat root "b.bin")))
      ;; An ignored name would be invisible, so it is not left behind.
      (should-error (mevedel-collaboration-files--finish-upload
                     root (funcall upload "run.log" '("x") 1)))
      (should-not (file-exists-p (file-name-concat root "run.log"))))))

(mevedel-deftest mevedel-collaboration-files-handle-upload
  (:doc "acknowledges chunks, writes the file, and resets on refusal")
  (mevedel-collaboration-files-test--with-root root
    (mevedel-collaboration-files-test--write root "docs/a.md")
    (let* ((owner (mevedel-collaboration-files-test--owner))
           (guest (gethash 2 (plist-get owner :guests)))
           sent)
      (mevedel-collaboration-files-test--capturing sent
        (let ((chunk (lambda (peer frame)
                       (setq sent nil)
                       (mevedel-collaboration-files-handle-upload
                        owner peer frame root)
                       (cl-find-if (lambda (entry)
                                     (equal "file-upload"
                                            (plist-get (cdr entry) :t)))
                                   sent))))
          (should (equal '(2 :t "file-upload" :reqId 1 :ok t :received 3)
                         (funcall chunk 2 (list :reqId 1 :dir "docs"
                                                :name "b.txt" :size 6
                                                :data (base64-encode-string
                                                       "abc")))))
          (should (equal '(2 :t "file-upload" :reqId 1 :ok t
                             :path "docs/b.txt")
                         (funcall chunk 2 (list :reqId 1 :final t
                                                :data (base64-encode-string
                                                       "def")))))
          (should (member '(2 :t "files-changed" :dir "docs") sent))
          (should-not (plist-get guest :upload))
          (should (equal "abcdef"
                         (with-temp-buffer
                           (insert-file-contents
                            (file-name-concat root "docs/b.txt"))
                           (buffer-string))))
          ;; A chunk over the announced size ends the upload.
          (funcall chunk 2 (list :reqId 2 :dir "" :name "c.txt" :size 1
                                 :data (base64-encode-string "too long")))
          (should (string-match-p "larger"
                                  (plist-get (cdar sent) :error)))
          (should-not (plist-get guest :upload))
          ;; So does a continuation without data, and a stray chunk of an
          ;; upload the host no longer holds does not start one.
          (funcall chunk 2 (list :reqId 3 :dir "" :name "d.txt" :size 2
                                 :data (base64-encode-string "x")))
          (should (equal 3 (plist-get (plist-get guest :upload) :req-id)))
          (should (plist-get (cdr (funcall chunk 2 '(:reqId 3 :data 7)))
                             :error))
          (should-not (plist-get guest :upload))
          (should (plist-get (cdr (funcall chunk 2 '(:reqId 3 :data "eA==")))
                             :error))
          (should-not (file-exists-p (file-name-concat root "d.txt")))
          (should (string-match-p
                   "view link"
                   (plist-get (cdr (funcall chunk 1 (list :reqId 4 :dir ""
                                                          :name "e.txt" :size 1
                                                          :data "eA==")))
                              :error))))))))

(mevedel-deftest mevedel-collaboration-files-handle-remove
  (:doc "trashes listed files and keeps ones with unsaved Emacs edits")
  (mevedel-collaboration-files-test--with-root root
    (let* ((trash-directory (file-name-concat root ".mevedel" "trash"))
           (owner (mevedel-collaboration-files-test--owner))
           (kept (mevedel-collaboration-files-test--write root "kept.txt"))
           (buffer (find-file-noselect kept))
           sent)
      (mevedel-collaboration-files-test--write root "docs/a.md")
      (unwind-protect
          (mevedel-collaboration-files-test--capturing sent
            (mevedel-collaboration-files-handle-remove
             owner 2 '(:reqId 1 :path "docs/a.md") root)
            (should (member '(2 :t "file-remove" :reqId 1 :ok t
                                :path "docs/a.md")
                            sent))
            (should (member '(2 :t "files-changed" :dir "docs") sent))
            (should-not (file-exists-p (file-name-concat root "docs/a.md")))
            (should (file-exists-p (file-name-concat trash-directory "a.md")))
            (with-current-buffer buffer (insert "edit"))
            (setq sent nil)
            (mevedel-collaboration-files-handle-remove
             owner 2 '(:reqId 2 :path "kept.txt") root)
            (should (string-match-p "unsaved" (plist-get (cdar sent) :error)))
            (should (file-exists-p kept))
            (setq sent nil)
            (mevedel-collaboration-files-handle-remove
             owner 1 '(:reqId 3 :path "kept.txt") root)
            (should (plist-get (cdar sent) :error))
            (should (file-exists-p kept)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(mevedel-deftest mevedel-collaboration--send-chunked
  (:doc "splits content into final-flagged frames under the wire bound")
  (let (sent)
    (mevedel-collaboration-files-test--capturing sent
      (let ((mevedel-collaboration--max-frame-json-bytes 200))
        (should (mevedel-collaboration--send-chunked
                 'transport 3 '(:t "file" :reqId 1) (make-string 500 ?y)))))
    (setq sent (nreverse sent))
    (should (> (length sent) 1))
    (dolist (entry sent)
      (should (<= (string-bytes (json-encode (cdr entry))) 200)))
    (should (equal (make-string 500 ?y)
                   (base64-decode-string
                    (mapconcat (lambda (entry) (plist-get (cdr entry) :data))
                               sent))))
    (setq sent nil)
    (mevedel-collaboration-files-test--capturing sent
      (mevedel-collaboration--send-chunked 'transport 3 '(:t "file") ""))
    (should (equal '((3 :t "file" :data "" :final t)) sent))))

(mevedel-deftest mevedel-collaboration--utf8-text-p
  (:doc "accepts UTF-8 text and refuses NUL bytes and invalid sequences")
  (progn
    (should (mevedel-collaboration--utf8-text-p "plain"))
    (should (mevedel-collaboration--utf8-text-p
             (encode-coding-string "größe ✓" 'utf-8)))
    (should-not (mevedel-collaboration--utf8-text-p "a\0b"))
    (should-not (mevedel-collaboration--utf8-text-p "\377\376"))))

(provide 'test-mevedel-collaboration-files)
;;; test-mevedel-collaboration-files.el ends here
