;;; test-niwa-collaboration-files.el --- Browser project file tests -*- lexical-binding: t; -*-

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
(require 'niwa-collaboration-files)
(require 'niwa-collaboration)
(require 'niwa-collaboration-guest)
(require 'niwa-collaboration-transport)

(defmacro niwa-collaboration-files-test--with-root (root &rest body)
  "Bind ROOT to a fresh temporary project directory around BODY."
  (declare (indent 1))
  `(let ((,root (file-name-as-directory
                 (make-temp-file "niwa-files-" t))))
     (unwind-protect (progn ,@body)
       (delete-directory ,root t))))

(defun niwa-collaboration-files-test--write (root path &optional content)
  "Write CONTENT, default PATH itself, to PATH under ROOT."
  (let ((file (file-name-concat root path)))
    (make-directory (file-name-directory file) t)
    (let ((coding-system-for-write 'binary))
      (write-region (or content path) nil file nil 'silent))
    file))

(defun niwa-collaboration-files-test--owner ()
  "Return a lobby-like owner with a viewer (1) and a full guest (2)."
  (let ((guests (make-hash-table :test #'eql)))
    (puthash 1 (list :name "viewer" :writable nil) guests)
    (puthash 2 (list :name "writer" :writable t) guests)
    (list :transport 'transport :guests guests)))

(defmacro niwa-collaboration-files-test--capturing (sent &rest body)
  "Run BODY with sent frames pushed onto SENT as (PEER . FRAME)."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'niwa-collaboration--transport-send)
              (lambda (_transport peer frame)
                (push (cons peer frame) ,sent)
                t)))
     ,@body))

(niwa-deftest niwa-collaboration-files--listing
  (:doc "lists project files without workspace state, following ignore rules")
  (progn
    (niwa-collaboration-files-test--with-root root
      (niwa-collaboration-files-test--write root "notes.md")
      (niwa-collaboration-files-test--write root "data/table.csv")
      (niwa-collaboration-files-test--write root ".niwa/lobby")
      (niwa-collaboration-files-test--write root ".niwa/shared/x.md")
      (niwa-collaboration-files-test--write root "app/.niwa/lobby")
      (niwa-collaboration-files-test--write root "app/.niwa/permissions.el")
      (should (equal '("data/table.csv" "notes.md")
                     (niwa-collaboration-files--listing root))))
    (niwa-collaboration-files-test--with-root root
      (let ((default-directory root))
        (should (zerop (call-process "git" nil nil nil "init" "--quiet"))))
      (niwa-collaboration-files-test--write root ".gitignore" "*.log\n")
      (niwa-collaboration-files-test--write root "src/main.py")
      (niwa-collaboration-files-test--write root "debug.log")
      (niwa-collaboration-files-test--write root ".niwa/shared/x.md")
      (should (equal '(".gitignore" "src/main.py")
                     (niwa-collaboration-files--listing root)))
      ;; A user's own project backend, possibly cached, is not consulted.
      (let ((project-find-functions
             (list (lambda (_dir) (cons 'transient "/nowhere/")))))
        (should (equal '(".gitignore" "src/main.py")
                       (niwa-collaboration-files--listing root)))))))

(defun niwa-collaboration-files-test--git-init (dir)
  "Make DIR a Git repository."
  (make-directory dir t)
  (let ((default-directory dir))
    (should (zerop (call-process "git" nil nil nil "init" "--quiet")))))

(niwa-deftest niwa-collaboration-files--nested-repositories
  (:doc "lists untracked nested Git repositories by their own rules")
  (niwa-collaboration-files-test--with-root root
    (niwa-collaboration-files-test--git-init root)
    (niwa-collaboration-files-test--write root ".gitignore" "ignored/\n")
    (niwa-collaboration-files-test--write root "top.txt")
    (niwa-collaboration-files-test--write root "plain/b.txt")
    (niwa-collaboration-files-test--git-init (file-name-concat root ".niwa/refs/private"))
    (niwa-collaboration-files-test--write root ".niwa/refs/private/secret.txt")
    ;; A reference clone, with its own ignores, workspace state and a
    ;; clone nested in it in turn.
    (niwa-collaboration-files-test--git-init
     (file-name-concat root "refs/app"))
    (niwa-collaboration-files-test--write root "refs/app/.gitignore"
                                             "*.log\n")
    (niwa-collaboration-files-test--write root "refs/app/src/main.py")
    (niwa-collaboration-files-test--write root "refs/app/debug.log")
    (niwa-collaboration-files-test--write root "refs/app/.niwa/lobby")
    (niwa-collaboration-files-test--git-init
     (file-name-concat root "refs/app/vendor/lib"))
    (niwa-collaboration-files-test--write root "refs/app/vendor/lib/x.el")
    ;; A clone the project ignores stays hidden.
    (niwa-collaboration-files-test--git-init
     (file-name-concat root "ignored/repo"))
    (niwa-collaboration-files-test--write root "ignored/repo/secret.txt")
    (should (equal '(".gitignore" "plain/b.txt" "refs/app/.gitignore"
                     "refs/app/src/main.py" "refs/app/vendor/lib/x.el"
                     "top.txt")
                   (niwa-collaboration-files--listing root)))
    ;; Without untracked material, the clones are left out like any.
    (let ((default-directory root))
      (should (zerop (call-process "git" nil nil nil "add" ".gitignore"
                                   "top.txt"))))
    (let ((project-vc-include-untracked nil))
      (should (equal '(".gitignore" "top.txt")
                     (niwa-collaboration-files--listing root))))
    ;; The listing is what lets a guest name the clone's files.
    (should (equal (file-name-concat root "refs/app/src/main.py")
                   (niwa-collaboration-files--file
                    root (niwa-collaboration-files--listing root)
                    "refs/app/src/main.py")))))

(niwa-deftest niwa-collaboration-files-private-access
  (:doc "nested workspace state authorizes no read, upload or removal")
  (niwa-collaboration-files-test--with-root root
    (let ((path "app/.niwa/lobby")
          (owner (niwa-collaboration-files-test--owner)) sent trashed)
      (niwa-collaboration-files-test--write root path "private credentials")
      (niwa-collaboration-files-test--write root "app/main.el")
      (niwa-collaboration-files-test--capturing sent
        (niwa-collaboration-files-handle-get owner 2 (list :reqId 1 :path path) root)
        (should (plist-get (cdar sent) :error))
        (should-error (niwa-collaboration-files--begin-upload
                       root '(:dir "app/.niwa" :name "upload.txt" :size 1) 2))
        (cl-letf (((symbol-function 'move-file-to-trash)
                   (lambda (&rest _) (setq trashed t))))
          (setq sent nil)
          (niwa-collaboration-files-handle-remove owner 2 (list :reqId 3 :path path) root)
          (should (plist-get (cdar sent) :error))
          (should-not trashed))))))

(niwa-deftest niwa-collaboration-files--folder-p
  (:doc "accepts the root and folders that hold listed files only")
  (let ((listing '("a/b/c.txt" "ab.txt")))
    (should (niwa-collaboration-files--folder-p listing ""))
    (should (niwa-collaboration-files--folder-p listing "a"))
    (should (niwa-collaboration-files--folder-p listing "a/b"))
    (should-not (niwa-collaboration-files--folder-p listing "a/"))
    (should-not (niwa-collaboration-files--folder-p listing "ab.txt"))
    (should-not (niwa-collaboration-files--folder-p listing "../a"))
    (should-not (niwa-collaboration-files--folder-p listing nil))))

(niwa-deftest niwa-collaboration-files--entries
  (:doc "lists one folder's children, folders first, skipping stale names")
  (niwa-collaboration-files-test--with-root root
    (niwa-collaboration-files-test--write root "a/x.txt")
    (niwa-collaboration-files-test--write root "a/y/z.txt")
    (niwa-collaboration-files-test--write root "b.txt" "four")
    (niwa-collaboration-files-test--write root "outside")
    (make-symbolic-link (file-name-concat root "outside")
                        (file-name-concat root "link.txt"))
    (let ((listing '("a/x.txt" "a/y/z.txt" "b.txt" "gone.txt" "link.txt")))
      (should (equal '(((:name "a" :kind "dir")
                        (:name "b.txt" :kind "file" :size 4))
                       . 0)
                     (niwa-collaboration-files--entries root listing "")))
      (should (equal '(((:name "y" :kind "dir")
                        (:name "x.txt" :kind "file" :size 7))
                       . 0)
                     (niwa-collaboration-files--entries root listing "a")))
      (let ((niwa-collaboration-files--max-entries 1))
        (should (equal 1 (cdr (niwa-collaboration-files--entries
                               root listing ""))))))))

(niwa-deftest niwa-collaboration-files--file
  (:doc "resolves listed plain files and refuses everything else")
  (niwa-collaboration-files-test--with-root root
    (let ((file (niwa-collaboration-files-test--write root "a.txt")))
      (niwa-collaboration-files-test--write root "private.txt")
      (make-symbolic-link file (file-name-concat root "link.txt"))
      (let ((listing '("a.txt" "link.txt" "gone.txt")))
        (should (equal file (niwa-collaboration-files--file
                             root listing "a.txt")))
        (dolist (path '("private.txt" "link.txt" "gone.txt" "../a.txt" nil))
          (should-error (niwa-collaboration-files--file
                         root listing path)))))))

(niwa-deftest niwa-collaboration-files--name-p
  (:doc "accepts one visible path component only")
  (progn
    (should (niwa-collaboration-files--name-p "report 2.pdf"))
    (should (niwa-collaboration-files--name-p "notes.md"))
    (dolist (name '("" "a/b" "a\\b" ".env" "~root" "tab\tname" nil))
      (should-not (niwa-collaboration-files--name-p name)))
    (should-not (niwa-collaboration-files--name-p (make-string 256 ?x)))))

(niwa-deftest niwa-collaboration-files--parent
  (:doc "names a path's folder, the root as the empty string")
  (progn
    (should (equal "" (niwa-collaboration-files--parent "a.txt")))
    (should (equal "a/b" (niwa-collaboration-files--parent "a/b/c.txt")))))

(niwa-deftest niwa-collaboration-files--reply
  (:doc "addresses a typed answer to one peer")
  (let (sent)
    (niwa-collaboration-files-test--capturing sent
      (niwa-collaboration-files--reply
       '(:transport transport) 4 "files" 9 '(:ok t)))
    (should (equal '((4 :t "files" :reqId 9 :ok t)) sent))))

(niwa-deftest niwa-collaboration-files--announce
  (:doc "tells writable guests only")
  (let ((owner (niwa-collaboration-files-test--owner))
        sent)
    (niwa-collaboration-files-test--capturing sent
      (niwa-collaboration-files--announce owner "docs"))
    (should (equal '((2 :t "files-changed" :dir "docs")) sent))))

(niwa-deftest niwa-collaboration-files--mime
  (:doc "types known files and previews untyped UTF-8 text as plain text")
  (progn
    (should (equal "text/markdown"
                   (niwa-collaboration-files--mime "a.md" "# x")))
    (should (equal "text/plain"
                   (niwa-collaboration-files--mime "a.el" "(provide 'a)")))
    (should (equal "application/octet-stream"
                   (niwa-collaboration-files--mime "a.bin" "\0\1")))))

(niwa-deftest niwa-collaboration-files-handle-list
  (:doc "answers full links with a folder listing and refuses the rest")
  (niwa-collaboration-files-test--with-root root
    (niwa-collaboration-files-test--write root "docs/a.md")
    (let ((owner (niwa-collaboration-files-test--owner))
          sent)
      (niwa-collaboration-files-test--capturing sent
        (let ((ask (lambda (peer frame)
                     (setq sent nil)
                     (niwa-collaboration-files-handle-list
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
                         (plist-get (funcall ask 2 '(:reqId 3 :dir ".niwa"))
                                    :error)))
          (should (string-match-p
                   "view link"
                   (plist-get (funcall ask 1 '(:reqId 4 :dir "")) :error)))
          ;; Unknown peers and malformed request ids are not answered.
          (should-not (funcall ask 7 '(:reqId 5 :dir "")))
          (should-not (funcall ask 2 '(:reqId "x" :dir ""))))))))

(niwa-deftest niwa-collaboration-files-handle-get
  (:doc "streams listed files in chunks and refuses unlisted ones")
  (niwa-collaboration-files-test--with-root root
    (let ((content (make-string 2000 ?x))
          (owner (niwa-collaboration-files-test--owner))
          sent)
      (niwa-collaboration-files-test--write root "src/main.py" content)
      (niwa-collaboration-files-test--write root ".niwa/lobby" "secret")
      (niwa-collaboration-files-test--capturing sent
        (let ((niwa-collaboration--max-frame-json-bytes 800))
          (niwa-collaboration-files-handle-get
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
        (dolist (case '((2 . ".niwa/lobby") (2 . "../x") (1 . "src/main.py")))
          (setq sent nil)
          (niwa-collaboration-files-handle-get
           owner (car case) (list :reqId 4 :path (cdr case)) root)
          (should (= 1 (length sent)))
          (should (stringp (plist-get (cdar sent) :error)))
          (should-not (plist-get (cdar sent) :data)))
        (let ((niwa-collaboration--max-artifact-bytes 100))
          (setq sent nil)
          (niwa-collaboration-files-handle-get
           owner 2 '(:reqId 5 :path "src/main.py") root)
          (should (string-match-p "too large"
                                  (plist-get (cdar sent) :error))))))))

(niwa-deftest niwa-collaboration-files--begin-upload
  (:doc "admits a free plain name in a listed folder within the bound")
  (niwa-collaboration-files-test--with-root root
    (niwa-collaboration-files-test--write root "docs/a.md")
    (should (equal '(:req-id 1 :path "docs/b.md" :size 3 :received 0 :parts nil)
                   (niwa-collaboration-files--begin-upload
                    root '(:dir "docs" :name "b.md" :size 3) 1)))
    (should (equal "b.md"
                   (plist-get (niwa-collaboration-files--begin-upload
                               root '(:dir "" :name "b.md" :size 0) 1)
                              :path)))
    ;; A share from a prompt takes the next free name instead.
    (should (equal "docs/a-2.md"
                   (plist-get (niwa-collaboration-files--begin-upload
                               root '(:dir "docs" :name "a.md" :size 1
                                           :rename t)
                               1)
                              :path)))
    (dolist (frame `((:dir "nowhere" :name "b.md" :size 3)
                     (:dir ".niwa" :name "b.md" :size 3)
                     (:dir "docs" :name ".env" :size 3)
                     (:dir "docs" :name "a.md" :size 3)
                     (:dir "docs" :name "b.md" :size -1)
                     (:dir "docs" :name "b.md"
                           :size ,(1+ niwa-collaboration-files--max-upload-bytes))))
      (should-error (niwa-collaboration-files--begin-upload root frame 1)))))

(niwa-deftest niwa-collaboration-files--free-path
  (:doc "keeps a free name and numbers a taken one")
  (niwa-collaboration-files-test--with-root root
    (niwa-collaboration-files-test--write root "a.png")
    (niwa-collaboration-files-test--write root "a-2.png")
    (niwa-collaboration-files-test--write root "Makefile")
    (should (equal "b.png" (niwa-collaboration-files--free-path root "b.png")))
    (should (equal "a-3.png" (niwa-collaboration-files--free-path root "a.png")))
    (should (equal "Makefile-2"
                   (niwa-collaboration-files--free-path root "Makefile")))))

(niwa-deftest niwa-collaboration-files--finish-upload
  (:doc "writes a new file once and refuses short, taken or ignored ones")
  (niwa-collaboration-files-test--with-root root
    (let ((default-directory root))
      (should (zerop (call-process "git" nil nil nil "init" "--quiet"))))
    (niwa-collaboration-files-test--write root ".gitignore" "*.log\n")
    (let ((upload (lambda (path parts size)
                    (list :req-id 1 :path path :size size :parts parts))))
      (should (equal "a.bin" (niwa-collaboration-files--finish-upload
                              root (funcall upload "a.bin" '("\2\3" "\0\1") 4))))
      (should (equal "\0\1\2\3"
                     (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally
                        (file-name-concat root "a.bin"))
                       (buffer-string))))
      (should-error (niwa-collaboration-files--finish-upload
                     root (funcall upload "a.bin" '("x") 1)))
      (should-error (niwa-collaboration-files--finish-upload
                     root (funcall upload "b.bin" '("x") 2)))
      (should-not (file-exists-p (file-name-concat root "b.bin")))
      ;; An ignored name would be invisible, so it is not left behind.
      (should-error (niwa-collaboration-files--finish-upload
                     root (funcall upload "run.log" '("x") 1)))
      (should-not (file-exists-p (file-name-concat root "run.log"))))))

(niwa-deftest niwa-collaboration-files-handle-upload
  (:doc "acknowledges chunks, writes the file, and resets on refusal")
  (niwa-collaboration-files-test--with-root root
    (niwa-collaboration-files-test--write root "docs/a.md")
    (let* ((owner (niwa-collaboration-files-test--owner))
           (guest (gethash 2 (plist-get owner :guests)))
           sent)
      (niwa-collaboration-files-test--capturing sent
        (let ((chunk (lambda (peer frame)
                       (setq sent nil)
                       (niwa-collaboration-files-handle-upload
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

(niwa-deftest niwa-collaboration-files-handle-remove
  (:doc "trashes listed files and keeps ones with unsaved Emacs edits")
  (niwa-collaboration-files-test--with-root root
    (let* ((trash-directory (file-name-concat root ".niwa" "trash"))
           (owner (niwa-collaboration-files-test--owner))
           (kept (niwa-collaboration-files-test--write root "kept.txt"))
           (buffer (find-file-noselect kept))
           sent)
      (niwa-collaboration-files-test--write root "docs/a.md")
      (unwind-protect
          (niwa-collaboration-files-test--capturing sent
            (niwa-collaboration-files-handle-remove
             owner 2 '(:reqId 1 :path "docs/a.md") root)
            (should (member '(2 :t "file-remove" :reqId 1 :ok t
                                :path "docs/a.md")
                            sent))
            (should (member '(2 :t "files-changed" :dir "docs") sent))
            (should-not (file-exists-p (file-name-concat root "docs/a.md")))
            (should (file-exists-p (file-name-concat trash-directory "a.md")))
            (with-current-buffer buffer (insert "edit"))
            (setq sent nil)
            (niwa-collaboration-files-handle-remove
             owner 2 '(:reqId 2 :path "kept.txt") root)
            (should (string-match-p "unsaved" (plist-get (cdar sent) :error)))
            (should (file-exists-p kept))
            (setq sent nil)
            (niwa-collaboration-files-handle-remove
             owner 1 '(:reqId 3 :path "kept.txt") root)
            (should (plist-get (cdar sent) :error))
            (should (file-exists-p kept)))
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer)))))

(niwa-deftest niwa-collaboration--send-chunked
  (:doc "splits content into final-flagged frames under the wire bound")
  (let (sent)
    (niwa-collaboration-files-test--capturing sent
      (let ((niwa-collaboration--max-frame-json-bytes 200))
        (should (niwa-collaboration--send-chunked
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
    (niwa-collaboration-files-test--capturing sent
      (niwa-collaboration--send-chunked 'transport 3 '(:t "file") ""))
    (should (equal '((3 :t "file" :data "" :final t)) sent))))

(niwa-deftest niwa-collaboration--utf8-text-p
  (:doc "accepts UTF-8 text and refuses NUL bytes and invalid sequences")
  (progn
    (should (niwa-collaboration--utf8-text-p "plain"))
    (should (niwa-collaboration--utf8-text-p
             (encode-coding-string "größe ✓" 'utf-8)))
    (should-not (niwa-collaboration--utf8-text-p "a\0b"))
    (should-not (niwa-collaboration--utf8-text-p "\377\376"))))

(provide 'test-niwa-collaboration-files)
;;; test-niwa-collaboration-files.el ends here
